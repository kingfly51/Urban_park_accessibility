# -*- coding: utf-8 -*-
"""
Created on Sat Nov  1 21:59:00 2025

@author: Fei Wang
"""

import pandas as pd
import requests
import time
import re
import os
import json
from typing import List, Dict, Tuple, Optional
import logging
import math

# 设置日志
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class UrbanParkAccessibilityAnalyzer:
    def __init__(self, baidu_ak: str):
        """
        初始化城市公园可达性分析器
        
        Args:
            baidu_ak: 百度地图API的AK密钥
        """
        self.baidu_ak = baidu_ak
        self.park_types = ['公园']
        self.api_call_count = 0  # 记录API调用次数，用于监控但不限制
        self.max_parks_per_location = 20  # 每个地点最多计算多少个公园的步行距离
        
    def convert_gcj02_to_bd09(self, gcj_lng: float, gcj_lat: float) -> Tuple[float, float]:
        """
        将火星坐标系(GCJ-02)转换为百度坐标系(BD-09)
        使用百度官方坐标转换API
        """
        try:
            url = "http://api.map.baidu.com/geoconv/v1/"
            params = {
                'coords': f'{gcj_lng},{gcj_lat}',
                'from': 3,  # 3表示火星坐标系(GCJ-02)
                'to': 5,    # 5表示百度坐标系(BD-09)
                'ak': self.baidu_ak,
                'output': 'json'
            }
            
            logger.info(f"坐标转换: GCJ02({gcj_lng}, {gcj_lat}) -> BD09")
            response = requests.get(url, params=params, timeout=10)
            self.record_api_call("coordinate_conversion")
            result = response.json()
            
            if result.get('status') == 0 and result.get('result'):
                converted = result['result'][0]
                bd09_lng = converted['x']
                bd09_lat = converted['y']
                logger.info(f"坐标转换成功: BD09({bd09_lng}, {bd09_lat})")
                return bd09_lng, bd09_lat
            else:
                error_msg = result.get('message', '未知错误')
                logger.warning(f"坐标转换API失败: {error_msg}，使用原坐标")
                return gcj_lng, gcj_lat
                
        except Exception as e:
            logger.error(f"坐标转换错误: {e}，使用原坐标")
            return gcj_lng, gcj_lat
    
    def load_progress(self, progress_file: str) -> Dict:
        """
        加载进度文件
        """
        if os.path.exists(progress_file):
            try:
                with open(progress_file, 'r', encoding='utf-8') as f:
                    progress = json.load(f)
                logger.info(f"已加载进度文件，上次处理到第 {progress['last_processed_index'] + 1} 行")
                logger.info(f"已使用 API 调用次数: {progress['api_call_count']}")
                return progress
            except Exception as e:
                logger.error(f"加载进度文件失败: {e}")
        return {
            'last_processed_index': -1,
            'api_call_count': 0,
            'processed_addresses': {},
            'summary_results': [],
            'detail_results': []
        }
    
    def save_progress(self, progress_file: str, progress_data: Dict):
        """
        保存进度文件
        """
        try:
            with open(progress_file, 'w', encoding='utf-8') as f:
                json.dump(progress_data, f, ensure_ascii=False, indent=2)
            logger.info(f"进度已保存到: {progress_file}")
        except Exception as e:
            logger.error(f"保存进度文件失败: {e}")
    
    def record_api_call(self, call_type: str = "general"):
        """
        记录API调用
        """
        self.api_call_count += 1
        # 每50次API调用输出一次统计信息
        if self.api_call_count % 50 == 0:
            logger.info(f"已使用 API 调用次数: {self.api_call_count}")
    
    def extract_coordinates(self, address_text: str) -> Optional[Tuple[float, float]]:
        """
        从地址文本中提取经纬度坐标
        """
        try:
            patterns = [
                r'\[(\d+\.\d+)[,\s]*(\d+\.\d+)\]',
                r'\[(\d+\.\d+)[，\s]*(\d+\.\d+)\]',
                r'\[([\d\.]+)[,\s]*([\d\.]+)\]',
                r'\[([\d\.]+)[，\s]*([\d\.]+)\]',
                r'(\d+\.\d+)[,\s]*(\d+\.\d+)'
            ]
            
            for pattern in patterns:
                match = re.search(pattern, address_text)
                if match:
                    lng = float(match.group(1))
                    lat = float(match.group(2))
                    
                    if 73 <= lng <= 135 and 18 <= lat <= 54:
                        logger.info(f"成功提取坐标: GCJ02({lng}, {lat})")
                        return lng, lat
                    else:
                        logger.warning(f"坐标超出合理范围: ({lng}, {lat})")
            
            logger.warning(f"正则表达式匹配失败，尝试手动解析: {address_text}")
            return self._manual_coordinate_extraction(address_text)
                
        except Exception as e:
            logger.error(f"坐标提取错误: {e} - {address_text}")
            return None
    
    def _manual_coordinate_extraction(self, address_text: str) -> Optional[Tuple[float, float]]:
        """
        手动解析坐标（当正则表达式失败时使用）
        """
        try:
            start_idx = address_text.find('[')
            end_idx = address_text.find(']')
            
            if start_idx != -1 and end_idx != -1 and end_idx > start_idx:
                coord_text = address_text[start_idx + 1:end_idx]
                logger.info(f"手动提取坐标文本: '{coord_text}'")
                
                separators = ['，', ',', ' ', ';']
                for sep in separators:
                    parts = coord_text.split(sep)
                    if len(parts) == 2:
                        lng = float(parts[0].strip())
                        lat = float(parts[1].strip())
                        
                        if 73 <= lng <= 135 and 18 <= lat <= 54:
                            logger.info(f"手动解析成功: GCJ02({lng}, {lat})")
                            return lng, lat
                
                logger.warning(f"手动解析失败，无法分割坐标文本: '{coord_text}'")
            
            return None
            
        except Exception as e:
            logger.error(f"手动坐标提取错误: {e}")
            return None
    
    def search_nearby_parks(self, lng: float, lat: float, radius: int = 3000) -> List[Dict]:
        """
        搜索附近的公园（使用百度坐标系BD-09）
        """
        all_parks = []
        
        for park_type in self.park_types:
            try:
                url = "http://api.map.baidu.com/place/v2/search"
                params = {
                    'query': park_type,
                    'location': f'{lat},{lng}',  # 这里应该是BD-09坐标
                    'radius': radius,
                    'output': 'json',
                    'ak': self.baidu_ak,
                    'scope': 1  # 基础检索
                }
                
                logger.info(f"搜索公园类型: {park_type}")
                response = requests.get(url, params=params, timeout=10)
                self.record_api_call("place_search")
                result = response.json()
                
                # 检查API调用是否失败
                if result.get('status') != 0:
                    error_msg = result.get('message', '未知错误')
                    if '配额' in error_msg or '限额' in error_msg or 'qps' in error_msg.lower():
                        raise Exception(f"API配额不足: {error_msg}")
                    else:
                        logger.warning(f"搜索 {park_type} 失败: {error_msg}")
                        continue
                
                if result.get('results'):
                    logger.info(f"找到 {len(result['results'])} 个 {park_type}")
                    for place in result['results']:
                        park = {
                            'name': place.get('name'),
                            'type': park_type,
                            'location': place.get('location', {}),
                            'address': place.get('address'),
                            'straight_distance': place.get('detail_info', {}).get('distance'),
                            'uid': place.get('uid')
                        }
                        all_parks.append(park)
                else:
                    logger.info(f"未找到 {park_type}")
                
                time.sleep(0.1)
                
            except requests.exceptions.RequestException as e:
                logger.error(f"网络请求错误 {park_type}: {e}")
                raise Exception(f"网络请求失败: {e}")
            except Exception as e:
                if '配额' in str(e) or '限额' in str(e):
                    raise e  # 重新抛出配额错误
                logger.error(f"搜索公园错误 {park_type}: {e}")
                continue
        
        # 去重（基于UID）
        seen_uids = set()
        unique_parks = []
        for park in all_parks:
            if park['uid'] not in seen_uids:
                unique_parks.append(park)
                seen_uids.add(park['uid'])
        
        logger.info(f"在 BD09({lng}, {lat}) 附近找到 {len(unique_parks)} 个唯一公园")
        return unique_parks
    
    def calculate_walking_distance(self, origin_lng: float, origin_lat: float, 
                                 dest_lng: float, dest_lat: float) -> Optional[Dict]:
        """
        计算步行距离和时间（使用百度坐标系BD-09）
        """
        try:
            url = "http://api.map.baidu.com/direction/v2/walking"
            params = {
                'origin': f'{origin_lat},{origin_lng}',  # BD-09坐标
                'destination': f'{dest_lat},{dest_lng}',  # BD-09坐标
                'ak': self.baidu_ak,
                'output': 'json'
            }
            
            response = requests.get(url, params=params, timeout=10)
            self.record_api_call("direction")
            result = response.json()
            
            # 检查API调用是否失败
            if result.get('status') != 0:
                error_msg = result.get('message', '未知错误')
                if '配额' in error_msg or '限额' in error_msg or 'qps' in error_msg.lower():
                    raise Exception(f"API配额不足: {error_msg}")
                else:
                    logger.warning(f"路径规划失败: {error_msg}")
                    return None
            
            if result['result']['routes']:
                route = result['result']['routes'][0]
                return {
                    'distance': route['distance'],
                    'duration': route['duration']
                }
            else:
                logger.warning("路径规划返回空结果")
                return None
                
        except requests.exceptions.RequestException as e:
            logger.error(f"网络请求错误: {e}")
            raise Exception(f"网络请求失败: {e}")
        except Exception as e:
            if '配额' in str(e) or '限额' in str(e):
                raise e  # 重新抛出配额错误
            logger.error(f"计算步行距离错误: {e}")
            return None
    
    def calculate_park_accessibility(self, gcj02_lng: float, gcj02_lat: float) -> Dict:
        """
        计算城市公园可达性指标
        输入：火星坐标(GCJ-02)
        输出：可达性分析结果
        """
        logger.info(f"计算坐标 GCJ02({gcj02_lng}, {gcj02_lat}) 的公园可达性")
        
        # 1. 坐标转换：GCJ-02 -> BD-09
        bd09_lng, bd09_lat = self.convert_gcj02_to_bd09(gcj02_lng, gcj02_lat)
        
        # 2. 使用百度坐标搜索附近的公园
        parks = self.search_nearby_parks(bd09_lng, bd09_lat)
        
        if not parks:
            logger.info("未找到附近公园")
            return {
                'nearest_park_name': None,
                'nearest_park_type': None,
                'walking_distance_m': None,
                'walking_time_min': None,
                'park_count_500m': 0,
                'park_count_1000m': 0,
                'park_count_1500m': 0,
                'total_park_count': 0,
                'accessibility_level': 'No Access',
                'all_parks_info': []
            }
        
        # 3. 计算每个公园的步行距离
        accessible_parks = []
        max_parks_to_calculate = min(len(parks), self.max_parks_per_location)
        
        for i, park in enumerate(parks[:max_parks_to_calculate]):
            logger.info(f"计算第 {i+1}/{max_parks_to_calculate} 个公园的步行距离: {park['name']}")
            dest_lng = park['location']['lng']  # 公园坐标已经是BD-09
            dest_lat = park['location']['lat']
            
            walking_info = self.calculate_walking_distance(bd09_lng, bd09_lat, dest_lng, dest_lat)
            
            if walking_info:
                accessible_park = park.copy()
                accessible_park.update({
                    'walking_distance_m': walking_info['distance'],
                    'walking_time_min': round(walking_info['duration'] / 60, 1)
                })
                accessible_parks.append(accessible_park)
                logger.info(f"公园 '{park['name']}' 步行距离: {walking_info['distance']} 米")
            else:
                logger.warning(f"无法计算公园 '{park['name']}' 的步行距离")
            
            time.sleep(0.3)
        
        if not accessible_parks:
            logger.info("没有可达的公园")
            return {
                'nearest_park_name': None,
                'nearest_park_type': None,
                'walking_distance_m': None,
                'walking_time_min': None,
                'park_count_500m': 0,
                'park_count_1000m': 0,
                'park_count_1500m': 0,
                'total_park_count': 0,
                'accessibility_level': 'No Access',
                'all_parks_info': []
            }
        
        # 4. 按距离排序
        accessible_parks_sorted = sorted(accessible_parks, key=lambda x: x['walking_distance_m'])
        
        # 5. 提取所有公园的详细信息
        all_parks_info = []
        for i, park in enumerate(accessible_parks_sorted):
            park_info = {
                'park_rank': i + 1,
                'park_name': park['name'],
                'park_type': park['type'],
                'walking_distance_m': park['walking_distance_m'],
                'walking_time_min': park['walking_time_min'],
                'address': park.get('address', ''),
                'straight_distance': park.get('straight_distance')
            }
            all_parks_info.append(park_info)
        
        # 6. 找到步行距离最近的公园
        nearest_park = accessible_parks_sorted[0]
        
        # 7. 计算不同距离范围内的公园数量
        park_count_500m = len([p for p in accessible_parks_sorted if p['walking_distance_m'] <= 500])
        park_count_1000m = len([p for p in accessible_parks_sorted if p['walking_distance_m'] <= 1000])
        park_count_1500m = len([p for p in accessible_parks_sorted if p['walking_distance_m'] <= 1500])
        
        # 8. 确定可达性等级
        nearest_distance = nearest_park['walking_distance_m']
        if nearest_distance <= 300:
            accessibility_level = 'Excellent'
        elif nearest_distance <= 500:
            accessibility_level = 'Good'
        elif nearest_distance <= 1000:
            accessibility_level = 'Moderate'
        elif nearest_distance <= 1500:
            accessibility_level = 'Fair'
        else:
            accessibility_level = 'Poor'
        
        logger.info(f"可达性分析完成: 最近公园 '{nearest_park['name']}', 距离 {nearest_distance} 米, 等级 {accessibility_level}")
        logger.info(f"共找到 {len(accessible_parks_sorted)} 个可达公园")
        
        return {
            'nearest_park_name': nearest_park['name'],
            'nearest_park_type': nearest_park['type'],
            'walking_distance_m': nearest_park['walking_distance_m'],
            'walking_time_min': nearest_park['walking_time_min'],
            'park_count_500m': park_count_500m,
            'park_count_1000m': park_count_1000m,
            'park_count_1500m': park_count_1500m,
            'total_park_count': len(accessible_parks_sorted),
            'accessibility_level': accessibility_level,
            'all_parks_info': all_parks_info
        }
    
    def process_excel_file(self, input_file: str, output_file: str, address_column: str = '17、您现在的居住地：', 
                          progress_file: str = None, start_from: int = 0):
        """
        处理Excel文件，计算所有地址的城市公园可达性（支持断点续传）
        """
        if progress_file is None:
            progress_file = os.path.splitext(output_file)[0] + '_progress.json'
        
        # 加载进度
        progress = self.load_progress(progress_file)
        self.api_call_count = progress['api_call_count']
        
        try:
            # 读取Excel文件
            df = pd.read_excel(input_file)
            logger.info(f"成功读取文件，共 {len(df)} 行数据")
            
            # 初始化结果列表
            if progress['summary_results']:
                results = progress['summary_results']
                all_parks_details = progress['detail_results']
                logger.info(f"从进度文件恢复了 {len(results)} 条汇总结果和 {len(all_parks_details)} 条详情结果")
            else:
                results = []
                all_parks_details = []
            
            # 确定起始位置
            start_index = max(progress['last_processed_index'] + 1, start_from)
            if start_index > 0:
                logger.info(f"从第 {start_index + 1} 行开始处理")
            
            for index in range(start_index, len(df)):
                row = df.iloc[index]
                address_text = str(row[address_column])
                
                logger.info(f"处理第 {index + 1}/{len(df)} 行: {address_text}")
                
                # 检查是否已经处理过这个地址
                address_hash = hash(address_text)
                if str(address_hash) in progress['processed_addresses']:
                    logger.info(f"地址已处理过，跳过: {address_text}")
                    continue
                
                # 提取坐标（这里提取的是GCJ-02火星坐标）
                coordinates = self.extract_coordinates(address_text)
                
                if coordinates:
                    gcj02_lng, gcj02_lat = coordinates
                    
                    try:
                        # 计算公园可达性指标（内部会进行坐标转换）
                        accessibility_metrics = self.calculate_park_accessibility(gcj02_lng, gcj02_lat)
                        
                        result_row = {
                            'original_address': address_text,
                            'gcj02_longitude': gcj02_lng,  # 记录原始火星坐标
                            'gcj02_latitude': gcj02_lat,
                            'nearest_park_name': accessibility_metrics['nearest_park_name'],
                            'park_type': accessibility_metrics['nearest_park_type'],
                            'walking_distance_m': accessibility_metrics['walking_distance_m'],
                            'walking_time_min': accessibility_metrics['walking_time_min'],
                            'park_count_500m': accessibility_metrics['park_count_500m'],
                            'park_count_1000m': accessibility_metrics['park_count_1000m'],
                            'park_count_1500m': accessibility_metrics['park_count_1500m'],
                            'total_park_count': accessibility_metrics['total_park_count'],
                            'accessibility_level': accessibility_metrics['accessibility_level']
                        }
                        
                        # 保存所有公园的详细信息
                        for park_info in accessibility_metrics['all_parks_info']:
                            park_detail = {
                                'original_address': address_text,
                                'gcj02_longitude': gcj02_lng,  # 记录原始火星坐标
                                'gcj02_latitude': gcj02_lat,
                                'park_rank': park_info['park_rank'],
                                'park_name': park_info['park_name'],
                                'park_type': park_info['park_type'],
                                'walking_distance_m': park_info['walking_distance_m'],
                                'walking_time_min': park_info['walking_time_min'],
                                'park_address': park_info['address'],
                                'straight_distance': park_info.get('straight_distance')
                            }
                            all_parks_details.append(park_detail)
                        
                    except Exception as e:
                        if '配额' in str(e) or '限额' in str(e):
                            logger.error(f"API配额不足，停止处理: {e}")
                            # 保存当前进度
                            progress['last_processed_index'] = index - 1  # 回退到上一行
                            progress['api_call_count'] = self.api_call_count
                            progress['summary_results'] = results
                            progress['detail_results'] = all_parks_details
                            self.save_progress(progress_file, progress)
                            logger.info(f"进度已保存，API配额用尽，请稍后继续")
                            return results, all_parks_details
                        else:
                            # 其他错误，记录但继续处理
                            logger.error(f"处理地址 {address_text} 时出错: {e}")
                            result_row = {
                                'original_address': address_text,
                                'gcj02_longitude': gcj02_lng,
                                'gcj02_latitude': gcj02_lat,
                                'nearest_park_name': None,
                                'park_type': None,
                                'walking_distance_m': None,
                                'walking_time_min': None,
                                'park_count_500m': 0,
                                'park_count_1000m': 0,
                                'park_count_1500m': 0,
                                'total_park_count': 0,
                                'accessibility_level': 'Error'
                            }
                
                else:
                    result_row = {
                        'original_address': address_text,
                        'gcj02_longitude': None,
                        'gcj02_latitude': None,
                        'nearest_park_name': None,
                        'park_type': None,
                        'walking_distance_m': None,
                        'walking_time_min': None,
                        'park_count_500m': 0,
                        'park_count_1000m': 0,
                        'park_count_1500m': 0,
                        'total_park_count': 0,
                        'accessibility_level': 'No Access'
                    }
                
                results.append(result_row)
                
                # 更新进度
                progress['last_processed_index'] = index
                progress['api_call_count'] = self.api_call_count
                progress['processed_addresses'][str(address_hash)] = True
                progress['summary_results'] = results
                progress['detail_results'] = all_parks_details
                
                # 每处理完一行就保存进度
                self.save_progress(progress_file, progress)
                
                logger.info(f"已完成 {index + 1}/{len(df)} 行处理，API调用次数: {self.api_call_count}")
                
                # 避免请求过于频繁
                time.sleep(1)
            
            # 创建结果DataFrame
            result_df = pd.DataFrame(results)
            parks_detail_df = pd.DataFrame(all_parks_details)
            
            # 保存最终结果
            with pd.ExcelWriter(output_file, engine='openpyxl') as writer:
                result_df.to_excel(writer, sheet_name='可达性汇总', index=False)
                parks_detail_df.to_excel(writer, sheet_name='所有公园详情', index=False)
            
            logger.info(f"结果已保存到: {output_file}")
            logger.info(f"汇总数据: {len(result_df)} 行")
            logger.info(f"公园详情数据: {len(parks_detail_df)} 行")
            logger.info(f"总共使用 API 调用次数: {self.api_call_count}")
            
            # 删除进度文件（处理完成）
            if os.path.exists(progress_file):
                os.remove(progress_file)
                logger.info(f"进度文件已删除: {progress_file}")
            
            # 打印统计信息
            self.print_statistics(result_df, parks_detail_df)
            
            return result_df, parks_detail_df
            
        except Exception as e:
            logger.error(f"处理Excel文件错误: {e}")
            # 出错时保存进度
            self.save_progress(progress_file, progress)
            logger.info(f"进度已保存到: {progress_file}")
            raise
    
    def print_statistics(self, summary_df: pd.DataFrame, detail_df: pd.DataFrame):
        """打印统计信息"""
        valid_data = summary_df[summary_df['walking_distance_m'].notna()]
        
        if len(valid_data) > 0:
            print("\n=== 城市公园可达性统计 ===")
            print(f"有效数据量: {len(valid_data)}")
            print(f"平均步行距离: {valid_data['walking_distance_m'].mean():.1f} 米")
            print(f"平均步行时间: {valid_data['walking_time_min'].mean():.1f} 分钟")
            print(f"平均500米内公园数量: {valid_data['park_count_500m'].mean():.1f}")
            print(f"平均1000米内公园数量: {valid_data['park_count_1000m'].mean():.1f}")
            print(f"平均1500米内公园数量: {valid_data['park_count_1500m'].mean():.1f}")
            
            # 可达性等级分布
            level_counts = valid_data['accessibility_level'].value_counts()
            print(f"\n可达性等级分布:")
            for level, count in level_counts.items():
                percentage = (count / len(valid_data)) * 100
                print(f"{level}: {count} 个地点 ({percentage:.1f}%)")
            
            # 公园详情统计
            print(f"\n=== 公园详情统计 ===")
            print(f"总共找到公园记录: {len(detail_df)} 条")
            print(f"平均每个地点有 {len(detail_df) / len(valid_data):.1f} 个可达公园")
            
        else:
            print("\n=== 警告: 没有有效数据 ===")
            print("所有行的坐标提取都失败了")

# 使用示例
def main():
    # 替换为您的百度地图AK
    BAIDU_AK = "vhy5cUzfFIEw5I7rUrGMYe7TYCvAwgKH"
    
    # 初始化分析器
    analyzer = UrbanParkAccessibilityAnalyzer(BAIDU_AK)
    
    # 处理Excel文件
    input_file = "E:/Rdaima/green space Accessibility/sucide_people.xlsx"
    output_file = "E:/Rdaima/green space Accessibility/urban_park_accessibility_results.xlsx"
    progress_file = "E:/Rdaima/green space Accessibility/progress.json"
    
    try:
        summary_results, detail_results = analyzer.process_excel_file(
            input_file, output_file, progress_file=progress_file
        )
        print("处理完成！")
        
        print(f"\n=== 结果文件包含 ===")
        print(f"1. '可达性汇总' sheet: {len(summary_results)} 行")
        print(f"2. '所有公园详情' sheet: {len(detail_results)} 行")
        print(f"总共使用 API 调用次数: {analyzer.api_call_count}")
        
    except Exception as e:
        print(f"处理失败: {e}")
        print(f"进度已保存，下次运行会自动从断点继续")

if __name__ == "__main__":
    main()